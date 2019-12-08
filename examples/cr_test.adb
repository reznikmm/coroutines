--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Coroutines.Timeouts;

with CR_Test_Proc;

procedure CR_Test is
begin
   Coroutines.Initialize;
   Coroutines.Timeouts.Initialize;
   Coroutines.Start (CR_Test_Proc'Access, 40_000);
end CR_Test;
