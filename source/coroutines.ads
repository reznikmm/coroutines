--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------
--
--  Coroutine is a basic components for non-preemptive multitasking.
--

with System.Storage_Elements;
private with Ada.Containers.Vectors;

package Coroutines is

   pragma Preelaborate;

   procedure Initialize;
   --  Call this before use

   type Runable_Code is not null access procedure;
   --  A procedure to be executed as coroutine

   procedure Start
     (Runable    : Runable_Code;
      Stack_Size : System.Storage_Elements.Storage_Count);
   --  Launch a new coroutine.
   --  Runable is an actual code to be executed under the coroutine;
   --  it should not execute any blocking call (except Yield).
   --  Stack_Size is size of stack dedicated to execute the coroutine.

   generic
      type Argument_Type is private;
   procedure Generic_Start
     (Runable    : not null access procedure (Argument : Argument_Type);
      Stack_Size : System.Storage_Elements.Storage_Count;
      Argument   : Argument_Type);
   --  Generic version of Start procedure. Runable should be library-level
   --  procedure.

   procedure Yield;
   --  Runable procedure can call Yield to suspend execution. The coroutine
   --  stay ready for execution.

   type Event_Id is private;
   type Event_Id_Array is array (Positive range <>) of Event_Id;

   procedure Yield (Wait : Event_Id);
   --  Runable procadure can call Yield to suspend execution until Wait event
   --  happens.

   procedure Yield
     (Wait   : Event_Id_Array := (1 .. 0 => <>);
      Result : access Natural := null);
   --  Runable procadure can call Yield to suspend execution until one of event
   --  on Wait happens. On resume Result is an index of the event_id. If Wait
   --  is empty, then this Yield never returns.

private
   type Event_Object is limited interface;
   type Event_Id is access all Event_Object'Class;

   not overriding procedure Activate (Self : in out Event_Object) is abstract;

   not overriding function Ready
     (Self : Event_Object) return Boolean is abstract;

   not overriding procedure Deactivate
     (Self : in out Event_Object) is abstract;

   type Context is new System.Address;

   function Null_Context return Context is (Context (System.Null_Address));

   package Context_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Context);

   type Coroutine_Manager is limited interface;

   type Coroutine_Manager_Access is access all Coroutine_Manager'Class;

   not overriding procedure Get_Always_Ready_Event
     (Self   : in out Coroutine_Manager;
      Result : out Event_Id) is abstract;

   not overriding procedure New_Round
     (Self    : in out Coroutine_Manager;
      Queue   : in out Context_Vectors.Vector;
      Timeout : Duration) is null;

   Manager : Coroutine_Manager_Access;

   function Hash (Self : Context) return Ada.Containers.Hash_Type;

   function Current_Context return Context;

end Coroutines;
