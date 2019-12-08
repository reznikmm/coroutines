--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Address_To_Access_Conversions;
with Interfaces.C;

with Coroutines.Polling;

package body Coroutines is

   --  void * create_stack (int size)
   function create_context (size : Interfaces.C.int) return Context
     with Import, Convention => C, External_Name => "create_context";
   --  Allocate new stack with a header over it and guard page under it.
   --  Return pointer to the header

   --  init_context(context, code, arg1, arg2, cont)
   procedure init_context
     (Self     : Context;
      Wrapper  : System.Address;
      Code     : System.Address;
      Argument : System.Address;
      Cleanup  : Context)
     with Import, Convention => C, External_Name => "init_context";
   --  Initalize a context created by create_context function.
   --  When this context gets execution control first time it calls
   --  Wrapper (Code, Argument). When Wrapper returns it switches to
   --  Cleanup context. The last switch doesn't modify Current variable.

   procedure free_context (Object : Context)
     with Import, Convention => C, External_Name => "free_context";
   --  Destroy context created by create_context function

   --  switch_context(context *from, context *to)
   procedure swapcontext (prev, next : Context)
     with Import => True, Convention => C, External_Name => "switch_context";
   --  Suspend prev context execution and start executing next context

   procedure Destroyer (Unused : System.Address) with Convention => C;
   --  This procedure deallocates a finished coroutine. It takes context of
   --  the finished coroutine from Current variable.

   function Next_Context return Context;
   --  Find next coroutine to execute.

   procedure Run (Code, Unused : System.Address) with Convention => C;
   --  A low level wrapper to launch parameterless coroutine.

   type Destroy_Stack_Type is record
      Stack         : Wide_Wide_String (1 .. 256);
      Stack_Pointer : System.Address;
   end record;
   pragma No_Component_Reordering (Destroy_Stack_Type);

   Destroy_Stack : Destroy_Stack_Type;  --  A stack for Destroy coroutine
   Main_SP       : aliased System.Address;  --  A stack pointer for Main coro

   Main          : Context := Context (Main_SP'Address); --  Main coroutine
   Current       : Context;  --  Current coroutine
   Destroy       : Context := Context (Destroy_Stack.Stack_Pointer'Address);
   --  Cleanup coroutine

   Total         : Natural := 0;
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
      --  A low level wrapper to launch a coroutine.

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
      Next  : constant Context :=
        create_context (Interfaces.C.int (Stack_Size));
   begin
      init_context
        (Self     => Next,
         Wrapper  => Run'Address,
         Code     => Runable.all'Address,
         Argument => Copy'Address,
         Cleanup  => Destroy);

      Total := Total + 1;

      if Prev = Main then
         --  Dont activate main context after Start
         Current := Next;
         swapcontext (Prev, Current);
      else
         declare
            Ready : Event_Id;
         begin
            Manager.Get_Always_Ready_Event (Ready);
            Ready.Activate;
            Current := Next;
            swapcontext (Prev, Current);
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
         free_context (Current);
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
      Main := Context (Main_SP'Address);
      Destroy := Context (Destroy_Stack.Stack_Pointer'Address);
      Current := Main;

      init_context
        (Self     => Destroy,
         Wrapper  => Destroyer'Address,
         Code     => System.Null_Address,
         Argument => System.Null_Address,
         Cleanup  => Main);

      Coroutines.Polling.Initialize;
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

   -----------
   -- Start --
   -----------

   procedure Start
     (Runable    : Runable_Code;
      Stack_Size : System.Storage_Elements.Storage_Count)
   is
      Prev  : constant Context := Current;
      Next  : constant Context :=
        create_context (Interfaces.C.int (Stack_Size));
   begin
      init_context
        (Self     => Next,
         Wrapper  => Run'Address,
         Code     => Runable.all'Address,
         Argument => System.Null_Address,
         Cleanup  => Destroy);

      Total := Total + 1;

      if Prev = Main then
         --  Dont activate main context after Start
         Current := Next;
         swapcontext (Prev, Current);
      else
         declare
            Ready : Event_Id;
         begin
            Manager.Get_Always_Ready_Event (Ready);
            Ready.Activate;
            Current := Next;
            swapcontext (Prev, Current);
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
         swapcontext (Prev, Current);
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
