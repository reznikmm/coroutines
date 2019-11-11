--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Finalization;
with Interfaces.C;

package Coroutines.EPoll is

   procedure Initialize;

   subtype FD is Interfaces.C.int;

   type Event_Kind is (Input, Output, Error, Close);

   type Event_Kind_Set is new Interfaces.Unsigned_32;

   function "+" (Left : Event_Kind) return Event_Kind_Set;
   function "+"
     (Left : Event_Kind_Set; Right : Event_Kind) return Event_Kind_Set;
   function "and" (Left : Event_Kind_Set; Right : Event_Kind) return Boolean;

   type Mode is (Edge, Level);

   type Event
     (FD     : EPoll.FD;
      Events : Event_Kind_Set;
      Mode   : EPoll.Mode) is new Ada.Finalization.Limited_Controlled
       with private;

   procedure Track
     (Self  : in out Event'Class;
      Value : Boolean);
   --  If an event is tracked, then the current coroutive suspended until
   --  given file descriptor receives an event taking Mode into account.
   --  After next yield the coroutine won't be executed until the event. See
   --  'man epoll' for details. For now one coroutine track use only one file
   --  descriptor.

private

   type Event
     (FD     : EPoll.FD;
      Events : Event_Kind_Set;
      Mode   : EPoll.Mode) is new Ada.Finalization.Limited_Controlled
   with record
      Enabled : Boolean := False;
   end record;

   overriding procedure Finalize   (Self : in out Event);

end Coroutines.EPoll;
