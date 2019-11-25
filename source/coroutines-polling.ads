--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------
--
--  This package provides file descriptor polling events to yield.
--

with Interfaces.C;

package Coroutines.Polling is

   pragma Preelaborate;

   procedure Initialize;
   --  Call this before use

   subtype FD is Interfaces.C.int;

   type Polling_Kind is (Input, Output, Error, Close);

   type Polling_Kind_Set is array (Polling_Kind) of Boolean with Pack;

   function Watch
     (File   : FD;
      Events : Polling_Kind_Set) return Event_Id;
   --  Wake the coroutine up as state of given file descriptor changed.

end Coroutines.Polling;
