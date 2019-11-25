--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Address_To_Access_Conversions;
with Interfaces.C;

package body Coroutines is

   function Allocate_Context
     (Link       : Context;
      Stack_Size : Interfaces.C.size_t;
      Wrapper    : System.Address;
      Code       : System.Address;
      Argument   : System.Address) return Context
     with Import, Convention => C, External_Name => "allocate_context";

   procedure Free_Context (Object : Context)
     with Import, Convention => C, External_Name => "free_context";

   function swapcontext (prev, next : Context) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "swapcontext";

   procedure Destroyer (Unused : System.Address) with Convention => C;
   --  This procedure deallocates a finished coroutine

   function Next_Context return Context;

   Main    : Context;  --  Initial context
   Current : Context;  --  Current coroutine
   Destroy : Context;  --  Cleanup coroutine

   Total   : Natural := 0;
   --  Number of coroutines (excluding Main and Destroy)

   -------------------
   -- Generic_Start --
   -------------------

   procedure Generic_Start
     (Runable    : not null access procedure (Argument : Argument_Type);
      Stack_Size : System.Storage_Elements.Storage_Count;
      Argument   : Argument_Type)
   is
      package Convert is new System.Address_To_Access_Conversions
        (Object => Argument_Type);

      procedure Run (Code, Argument : System.Address) with Convention => C;

      ---------
      -- Run --
      ---------

      procedure Run (Code, Argument : System.Address) is
         procedure Proc (Argument : Argument_Type)
           with Import, Address => Code;
         --  Copy Argument to new stack
         Copy : constant Argument_Type := Convert.To_Pointer (Argument).all;
      begin
         Proc (Copy);

         --  return to Destroyer
      exception
         when others =>
            null;
      end Run;

      Prev  : constant Context := Current;
      Copy  : aliased Argument_Type := Argument;
      Next  : Context;
   begin
      Next := Allocate_Context
        (Link       => Destroy,
         Stack_Size => Interfaces.C.size_t (Stack_Size),
         Wrapper    => Run'Address,
         Code       => Runable.all'Address,
         Argument   => Copy'Address);

      Total := Total + 1;

      if Prev = Main then
         --  Dont activate main context after Start
         Current := Next;
         pragma Assert (swapcontext (Prev, Current) in 0);
      else
         declare
            Ready : Event_Id;
         begin
            Manager.Get_Always_Ready_Event (Ready);
            Ready.Activate;
            Current := Next;
            pragma Assert (swapcontext (Prev, Current) in 0);
            Ready.Deactivate;
         end;
      end if;
   end Generic_Start;

   ---------------
   -- Destroyer --
   ---------------

   procedure Destroyer (Unused : System.Address) is
   begin
      loop
         Free_Context (Current);
         Current := Destroy;
         Total := Total - 1;
         exit when Total = 0;
         Yield (Wait => (1 .. 0 => <>));
      end loop;

      --  Return to main context
   end Destroyer;

   ---------------------
   -- Current_Context --
   ---------------------

   function Current_Context return Context is
   begin
      return Current;
   end Current_Context;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Context) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type'Mod
        (System.Storage_Elements.To_Integer (System.Address (Self)));
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Main := Allocate_Context
        (Link       => Null_Context,
         Stack_Size => 0,
         Wrapper    => System.Null_Address,
         Code       => System.Null_Address,
         Argument   => System.Null_Address);

      Current := Main;

      Destroy := Allocate_Context
        (Link       => Main,
         Stack_Size => 4096,
         Wrapper    => Destroyer'Address,
         Code       => System.Null_Address,
         Argument   => System.Null_Address);
   end Initialize;

   Queue : Context_Vectors.Vector;

   ------------------
   -- Next_Context --
   ------------------

   function Next_Context return Context is
      Result : Context;
   begin
      while Queue.Is_Empty loop
         Manager.New_Round (Queue, Timeout => 300.0);
      end loop;

      Result := Queue.Last_Element;
      Queue.Delete_Last;

      return Result;
   end Next_Context;

   -----------
   -- Start --
   -----------

   procedure Start
     (Runable    : Runable_Code;
      Stack_Size : System.Storage_Elements.Storage_Count)
   is
      procedure Run (Code, Unused : System.Address) with Convention => C;

      ---------
      -- Run --
      ---------

      procedure Run (Code, Unused : System.Address) is
         procedure Proc with Import, Address => Code;
      begin
         Proc;

         --  return to Destroyer
      exception
         when others =>
            null;
      end Run;

      Prev  : constant Context := Current;
      Next  : Context;
   begin
      Next := Allocate_Context
        (Link       => Destroy,
         Stack_Size => Interfaces.C.size_t (Stack_Size),
         Wrapper    => Run'Address,
         Code       => Runable.all'Address,
         Argument   => System.Null_Address);

      Total := Total + 1;

      if Prev = Main then
         --  Dont activate main context after Start
         Current := Next;
         pragma Assert (swapcontext (Prev, Current) in 0);
      else
         declare
            Ready : Event_Id;
         begin
            Manager.Get_Always_Ready_Event (Ready);
            Ready.Activate;
            Current := Next;
            pragma Assert (swapcontext (Prev, Current) in 0);
            Ready.Deactivate;
         end;
      end if;
   end Start;

   -----------
   -- Yield --
   -----------

   procedure Yield is
      Ready : Event_Id;
   begin
      Manager.Get_Always_Ready_Event (Ready);
      Yield ((1 => Ready));
   end Yield;

   -----------
   -- Yield --
   -----------

   procedure Yield (Wait : Event_Id) is
   begin
      Yield ((1 => Wait));
   end Yield;

   -----------
   -- Yield --
   -----------

   procedure Yield
     (Wait   : Event_Id_Array := (1 .. 0 => <>);
      Result : access Natural := null)
   is
      Prev : constant Context := Current;
   begin
      for J of Wait loop
         J.Activate;
      end loop;

      Current := Next_Context;

      if Prev /= Current then
         pragma Assert (swapcontext (Prev, Current) in 0);
      end if;

      if Result /= null then
         Result.all := 0;

         for J in Wait'Range loop
            if Wait (J).Ready then
               Result.all := J;
               exit;
            end if;
         end loop;
      end if;

      for J of Wait loop
         J.Deactivate;
      end loop;
   end Yield;

end Coroutines;
