--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Text_IO;

with Coroutines.Timeouts;

procedure Timeout_Proc is
begin
   Ada.Text_IO.Put_Line
     (Ada.Calendar.Formatting.Image
        (Ada.Calendar.Clock));

   Coroutines.Yield (Coroutines.Timeouts.Timeout (5.0));

   Ada.Text_IO.Put_Line
     (Ada.Calendar.Formatting.Image
        (Ada.Calendar.Clock));
end Timeout_Proc;
