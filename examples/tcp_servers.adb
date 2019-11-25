--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Exceptions;
with Ada.Text_IO;

with Coroutines.Polling;

package body TCP_Servers is

   type Listener_Arguments is record
      Port  : GNAT.Sockets.Port_Type;
      Run   : not null TCP_Coroutine;
      Stack : System.Storage_Elements.Storage_Count;
   end record;

   procedure Listen (Args : Listener_Arguments);

   ------------
   -- Listen --
   ------------

   procedure Listen (Args : Listener_Arguments) is

      procedure Start is
        new Coroutines.Generic_Start (GNAT.Sockets.Socket_Type);

      Address    : GNAT.Sockets.Sock_Addr_Type :=
        (Family => GNAT.Sockets.Family_Inet,
         Addr   => GNAT.Sockets.Any_Inet_Addr,
         Port   => Args.Port);
      Server     : GNAT.Sockets.Socket_Type;
      Dont_Block : GNAT.Sockets.Request_Type :=
         (Name => GNAT.Sockets.Non_Blocking_IO,
          Enabled => True);
   begin
      GNAT.Sockets.Create_Socket (Server);
      GNAT.Sockets.Set_Socket_Option
        (Server,
         GNAT.Sockets.Socket_Level,
         (GNAT.Sockets.Reuse_Address, True));
      GNAT.Sockets.Control_Socket (Server, Dont_Block);
      GNAT.Sockets.Bind_Socket (Server, Address);

      declare
         Socket : GNAT.Sockets.Socket_Type;
      begin
         GNAT.Sockets.Listen_Socket (Server);

         loop
            --  Wait a Server socket event
            Coroutines.Yield
              (Coroutines.Polling.Watch
                 (Coroutines.Polling.FD (GNAT.Sockets.To_C (Server)),
                  (Coroutines.Polling.Input
                   | Coroutines.Polling.Error
                   | Coroutines.Polling.Close => True, others => False)));

            begin
               GNAT.Sockets.Accept_Socket (Server, Socket, Address);
               Start (Args.Run, Args.Stack, Socket);
            exception
               when E : GNAT.Sockets.Socket_Error =>
                  if GNAT.Sockets.Resolve_Exception (E) not in
                    GNAT.Sockets.Resource_Temporarily_Unavailable
                  then
                     Ada.Text_IO.Put_Line
                       ("Listen_Socket Error:"
                        & Ada.Exceptions.Exception_Message (E));
                     Ada.Text_IO.Put_Line
                       (Ada.Exceptions.Exception_Information (E));
                     exit;
                  end if;
            end;
         end loop;
      end;
   end Listen;

   -----------------
   -- Listen_Port --
   -----------------

   procedure Listen_Port
     (Port  : GNAT.Sockets.Port_Type;
      Run   : not null TCP_Coroutine;
      Stack : System.Storage_Elements.Storage_Count)
   is
      procedure Start is
        new Coroutines.Generic_Start (Listener_Arguments);

   begin
      Start (Listen'Access, Stack, (Port, Run, Stack));
   end Listen_Port;

end TCP_Servers;
