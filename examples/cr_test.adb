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

with Coroutines.EPoll;

procedure CR_Test is
   type HTTP_Client is new Coroutines.Runable with record
      Socket : GNAT.Sockets.Socket_Type;
   end record;

   overriding procedure Run (Self : in out HTTP_Client);

   function Get
     (Self : in out HTTP_Client'Class;
      URL  : String
--      ; Code : out Positive
     ) return Ada.Streams.Stream_Element_Array;

   ---------
   -- Get --
   ---------

   function Get
     (Self : in out HTTP_Client'Class;
      URL  : String
--      ; Code : out Positive
     ) return Ada.Streams.Stream_Element_Array
   is
      use type Ada.Streams.Stream_Element_Offset;

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
      GNAT.Sockets.Create_Socket (Self.Socket);
      GNAT.Sockets.Connect_Socket (Self.Socket, Address);
      GNAT.Sockets.Send_Socket (Self.Socket, Data, Last);
      pragma Assert (Last = Data'Last);

      declare
         use type Coroutines.EPoll.Event_Kind_Set;

         Socket_Input : Coroutines.EPoll.Event
           (Coroutines.EPoll.FD (GNAT.Sockets.To_C (Self.Socket)),
            Events => +Coroutines.EPoll.Input,
            Mode   => Coroutines.EPoll.Level);

         Output : Ada.Streams.Stream_Element_Array (1 .. 4096);
      begin
         Socket_Input.Track (True);
         --  Suspend current coroutine unti the socket has input.
         Coroutines.Yield;

         GNAT.Sockets.Receive_Socket (Self.Socket, Output, Last);
         --      Code := 200;
         Ada.Text_IO.Put_Line ("Last=" & (Last'Img));
         return Output (1 .. Last);
      end;
   end Get;

   overriding procedure Run (Self : in out HTTP_Client) is
      --      Code     : aliased Positive;
      Response : Ada.Streams.Stream_Element_Array :=
        Self.Get ("http://www.google.com/");  --  , Code);
      Text : String (1 .. Response'Length)
        with Import, Convention => Ada, Address => Response'Address;
   begin
      Ada.Text_IO.Put_Line (Text);
   end Run;

   R1 : aliased HTTP_Client;
begin
   Coroutines.EPoll.Initialize;

   declare
      C1 : aliased Coroutines.Coroutine
        (R1'Unchecked_Access,
         null,
         40_960);
   begin
      C1.Start;
      Coroutines.Switch_To (C1'Unchecked_Access);
   end;
end CR_Test;
