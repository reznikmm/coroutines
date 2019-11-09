--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Interfaces.C;

package Coroutines.EPoll is

   procedure Initialize;

   subtype FD is Interfaces.C.int;

   type Event is (Input, Output, Error, Close);

   type Mode is (Edge, Level);

   procedure Watch
     (Descriptor : FD;
      Event      : EPoll.Event;
      Mode       : EPoll.Mode);
   --  Mark current coroutive suspended until given file descriptor receives
   --  an event taking Mode into account. After next yield the coroutine
   --  won't be executed until the event. See 'man epoll' for details.
   --  For now one coroutine can use only one file descriptor.

   procedure Stop_Watch (Descriptor : FD);
   --  Don't associate current coroutine with given file descriptor any more.

end Coroutines.EPoll;
