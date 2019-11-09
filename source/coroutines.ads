--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Storage_Elements;
private with Ada.Finalization;
private with Ada.Containers;

package Coroutines is

   type Runable is limited interface;
   --  Runable object represents code to be executed under some coroutine.

   not overriding procedure Run (Self : in out Runable) is null;
   --  Run is actual procedure to be executed under some coroutine.
   --  Should not execute any blocking call (except Yield).

   type Runable_Access is access all Runable'Class;
   --  A reference to Runable

   procedure Yield;
   --  Run can call Yield to suspend execution.

   type Destructor is access procedure (Value : in out Runable_Access);
   --  Destructor for runable object

   type Coroutine
     (Runable    : not null Runable_Access;
      Destructor : Coroutines.Destructor;
      Stack_Size : System.Storage_Elements.Storage_Count)
        is tagged limited private;
   --  Coroutine is a basice components for non-preemptive multitasking.
   --  Runable is an actual code to be executed under the coroutine.
   --  Destructor (if not null) will be called at coroutine finalization.
   --  Stack_Size is size of stack dedicated to execute the coroutine.

   type Coroutine_Access is access all Coroutine'Class
     with Storage_Size => 0;

   function Current_Coroutine return not null Coroutine_Access;
   --  A reference to the coroutine that is currently running.

   procedure Start (Self : in out Coroutine'Class);
   --  Launch the coroutine. Could be called only once for given object.

   procedure Switch_To (Target : not null Coroutine_Access);
   --  Suspend execution of the current coroutine (like Yield) and switch to
   --  given coroutine.

private
   type Coroutine
     (Runable    : not null Runable_Access;
      Destructor : Coroutines.Destructor;
      Stack_Size : System.Storage_Elements.Storage_Count) is
        new Ada.Finalization.Limited_Controlled with
   record
      Started : Boolean := False;
      Context : System.Address;
      Stack   : System.Address;
   end record;

   type Coroutine_Manager is limited interface;

   type Coroutine_Manager_Access is access all Coroutine_Manager'Class;

   not overriding procedure Next_To_Run
     (Self  : in out Coroutine_Manager;
      Value : out Coroutine_Access) is abstract;

   procedure Register_Manager (Value : not null Coroutine_Manager_Access);
   --  Add one more Coroutine_Manager.

   function Hash (Self : Coroutine_Access) return Ada.Containers.Hash_Type;

end Coroutines;
