--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

with GNAT.Sockets;

with Coroutines.Polling;

pragma Style_Checks (Off);

procedure Server_Test_Proc (Socket : GNAT.Sockets.Socket_Type) is

   Dont_Block : GNAT.Sockets.Request_Type :=
     (Name    => GNAT.Sockets.Non_Blocking_IO,
      Enabled => True);

   Output : Ada.Streams.Stream_Element_Array (1 .. 256);
   Last   : Ada.Streams.Stream_Element_Offset;
   Text   : String (1 .. Output'Length)
     with Import, Address => Output'Address;
begin
   Ada.Text_IO.Put_Line
     ("Connected "
      & GNAT.Sockets.Image
        (GNAT.Sockets.Get_Peer_Name (Socket)));

   GNAT.Sockets.Control_Socket (Socket, Dont_Block);

   loop
      --  Suspend current coroutine unti the socket has input.
      Coroutines.Yield
        (Coroutines.Polling.Watch
           (Coroutines.Polling.FD (GNAT.Sockets.To_C (Socket)),
            (Coroutines.Polling.Input
             | Coroutines.Polling.Error
             | Coroutines.Polling.Close => True, others => False)));

      begin
         GNAT.Sockets.Receive_Socket (Socket, Output, Last);
         exit when Last in 0;
         Ada.Text_IO.Put_Line ("Last=" & (Last'Img));
         Ada.Text_IO.Put_Line (Text (1 .. Natural(Last)));
      exception
         when E : GNAT.Sockets.Socket_Error =>
            if GNAT.Sockets.Resolve_Exception (E) not in
              GNAT.Sockets.Resource_Temporarily_Unavailable
            then
               Ada.Text_IO.Put_Line
                 ("Listen_Socket Error:"
                  & Ada.Exceptions.Exception_Message (E));
               Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
               exit;
            end if;
      end;
   end loop;

   Ada.Text_IO.Put_Line
     ("Exit "
      & GNAT.Sockets.Image
        (GNAT.Sockets.Get_Peer_Name (Socket)));
end Server_Test_Proc;
