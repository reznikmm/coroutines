--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Coroutines;
with Timeout_Proc;
with Coroutines.Timeouts;

procedure Timeout_Test is
begin
   Coroutines.Initialize;
   Coroutines.Timeouts.Initialize;

   Coroutines.Start
     (Runable    => Timeout_Proc'Access,
      Stack_Size => 4096);
end Timeout_Test;
