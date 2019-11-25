--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Server_Test_Proc;

with Coroutines.Polling;
with Coroutines.Timeouts;

with TCP_Servers;

procedure Server_Test is
begin
   Coroutines.Initialize;
   Coroutines.Polling.Initialize;
   Coroutines.Timeouts.Initialize;

   TCP_Servers.Listen_Port
     (Port  => 12345,
      Run   => Server_Test_Proc'Access,
      Stack => 40_960);
end Server_Test;
