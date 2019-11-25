--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Storage_Elements;

with GNAT.Sockets;

package TCP_Servers is

   type TCP_Coroutine is access procedure (Socket : GNAT.Sockets.Socket_Type);

   procedure Listen_Port
     (Port  : GNAT.Sockets.Port_Type;
      Run   : not null TCP_Coroutine;
      Stack : System.Storage_Elements.Storage_Count);
   --  Start new coroutine, that will listen given Port and start Run coroutine
   --  on each connect. Use given Stack size for both kinds of coroutines.

end TCP_Servers;
