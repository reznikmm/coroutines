--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------
--
--  This package provides timeout events to yield.
--

with Ada.Calendar;

package Coroutines.Timeouts is

   procedure Initialize;
   --  Call this before use

   function Timeout (Value : Duration) return Event_Id;
   --  Wake the coroutine up after given timeout expired.

   function Timeout (Value : Ada.Calendar.Time) return Event_Id;
   --  Wake the coroutine up at given time.

end Coroutines.Timeouts;
