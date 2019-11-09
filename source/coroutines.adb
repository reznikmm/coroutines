--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Address_To_Access_Conversions;
with Interfaces.C;

package body Coroutines is

   function Allocate_Context
     (Link : System.Address;
      Stack_Size : Interfaces.C.size_t;
      Stack : System.Address;
      Value : System.Address) return System.Address
     with Import, Convention => C, External_Name => "allocate_context";

   function malloc (size : Interfaces.C.size_t) return System.Address
     with Import, Convention => C, External_Name => "malloc";

   procedure free (value : System.Address)
     with Import, Convention => C, External_Name => "free";
   pragma Unreferenced (free);

   function swapcontext
     (prev, next : System.Address) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "swapcontext";

   procedure Jump (Target : System.Address)
     with Export, Convention => C, External_Name => "jump_to_runable";

   package Runable_Addr is new System.Address_To_Access_Conversions
     (Object => Runable'Class);

   type No_Runable is new Runable with null record;
   overriding procedure Run (Self : in out No_Runable);

   ---------
   -- Run --
   ---------

   overriding procedure Run (Self : in out No_Runable) is
      pragma Unreferenced (Self);
   begin
      loop
         Yield;
      end loop;
   end Run;

   Dummy_Runable : aliased No_Runable;
   Dummy_Size    : constant := 4096;
   Dummy_Stack   : constant System.Address := malloc (Dummy_Size);
   Dummy         : aliased Coroutine :=
     (Ada.Finalization.Limited_Controlled with
      Runable => Dummy_Runable'Access,
      Destructor => null,
      Stack_Size => Dummy_Size,
      Started => True,
      Context => Allocate_Context
        (System.Null_Address,
         Dummy_Size,
         Dummy_Stack,
         Runable_Addr.To_Address (Dummy_Runable'Access)),
      Stack => Dummy_Stack);

   Main          : aliased Coroutine :=
     (Ada.Finalization.Limited_Controlled with
      Runable => Dummy_Runable'Access,
      Destructor => null,
      Stack_Size => 0,
      Started => True,
      Context => Allocate_Context
        (System.Null_Address,
         0,
         System.Null_Address,
         System.Null_Address),
      Stack => System.Null_Address);

   Current : not null Coroutine_Access := Main'Access;

   Manager : Coroutine_Manager_Access;

   -----------------------
   -- Current_Coroutine --
   -----------------------

   function Current_Coroutine return not null Coroutine_Access is
   begin
      return Current;
   end Current_Coroutine;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Coroutine_Access) return Ada.Containers.Hash_Type is
   begin
      if Self = null then
         return 0;
      else
         return Ada.Containers.Hash_Type'Mod
           (System.Storage_Elements.To_Integer (Self.all'Address));
      end if;
   end Hash;

   ----------
   -- Jump --
   ----------

   procedure Jump (Target : System.Address) is
      Object : constant Runable_Addr.Object_Pointer :=
        Runable_Addr.To_Pointer (Target);
   begin
      Object.Run;
   end Jump;

   ----------------------
   -- Register_Manager --
   ----------------------

   procedure Register_Manager (Value : not null Coroutine_Manager_Access) is
   begin
      Manager := Value;
   end Register_Manager;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Coroutine'Class) is
      Stack_Size : constant Interfaces.C.size_t :=
        Interfaces.C.size_t (Self.Stack_Size);
      Runable : constant Runable_Addr.Object_Pointer :=
        Runable_Addr.Object_Pointer (Self.Runable);
   begin
      pragma Assert (not Self.Started);

      Self.Stack := malloc (Stack_Size);
      Self.Context := Allocate_Context
        (Link       => Main.Context,
         Stack_Size => Stack_Size,
         Stack      => Self.Stack,
         Value      => Runable_Addr.To_Address (Runable));

      Self.Started := True;
   end Start;

   ---------------
   -- Switch_To --
   ---------------

   procedure Switch_To (Target : not null Coroutine_Access) is
      Prev : constant Coroutine_Access := Current;
   begin
      pragma Assert (Target.Started);
      Current := Target;
      pragma Assert (swapcontext (Prev.Context, Target.Context) in 0);
   end Switch_To;

   -----------
   -- Yield --
   -----------

   procedure Yield is
      Next : Coroutine_Access;
   begin
      Manager.Next_To_Run (Next);

      if Next = null then
         Next := Dummy'Access;
      end if;

      Switch_To (Next);
   end Yield;

end Coroutines;
