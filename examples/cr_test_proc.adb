--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.Sockets;

with Coroutines.Polling;

procedure CR_Test_Proc is

   function Get (URL : String) return Ada.Streams.Stream_Element_Array;
   --  Make HTTP GET request

   function Get (URL : String) return Ada.Streams.Stream_Element_Array is
      use type Ada.Streams.Stream_Element_Offset;

      Socket : GNAT.Sockets.Socket_Type;

      Slash : constant Natural := Ada.Strings.Fixed.Index
        (Source  => URL,
         Pattern => "/",
         From    => URL'First + 7);
      Host : constant String := URL (URL'First + 7 .. Slash - 1);
      Request : String := "GET " & URL (Slash .. URL'Last) & " HTTP/1.1"
        & Ada.Characters.Latin_1.CR
        & Ada.Characters.Latin_1.LF
        & "Host: " & Host
        & Ada.Characters.Latin_1.CR
        & Ada.Characters.Latin_1.LF
        & Ada.Characters.Latin_1.CR
        & Ada.Characters.Latin_1.LF;
      Data : Ada.Streams.Stream_Element_Array (1 .. Request'Length)
        with Import, Convention => Ada, Address => Request'Address;
      Last : Ada.Streams.Stream_Element_Offset;
      Address  : GNAT.Sockets.Sock_Addr_Type;
   begin
      Address.Addr := GNAT.Sockets.Addresses
        (GNAT.Sockets.Get_Host_By_Name (Host), 1);
      Address.Port := 80;
      GNAT.Sockets.Create_Socket (Socket);
      GNAT.Sockets.Connect_Socket (Socket, Address);
      GNAT.Sockets.Send_Socket (Socket, Data, Last);
      pragma Assert (Last = Data'Last);

      --  Suspend current coroutine unti the socket has input.
      Coroutines.Yield
        (Coroutines.Polling.Watch
           (File   => Coroutines.Polling.FD (GNAT.Sockets.To_C (Socket)),
            Events => (Coroutines.Polling.Input => True, others => False)));

      declare
         Output : Ada.Streams.Stream_Element_Array (1 .. 4096);
      begin
         GNAT.Sockets.Receive_Socket (Socket, Output, Last);
         --      Code := 200;
         Ada.Text_IO.Put_Line ("Last=" & (Last'Img));
         return Output (1 .. Last);
      end;
   end Get;

   Response : Ada.Streams.Stream_Element_Array :=
     Get ("http://www.google.com/");
   Text : String (1 .. Response'Length)
     with Import, Convention => Ada, Address => Response'Address;
begin
   Ada.Text_IO.Put_Line (Text);
end CR_Test_Proc;
