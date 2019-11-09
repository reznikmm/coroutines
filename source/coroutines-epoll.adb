--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Hashed_Maps;

package body Coroutines.EPoll is

   type Mapped_Data is record
      Descriptor : FD;
      Event      : EPoll.Event;
      Mode       : EPoll.Mode;
   end record;

   package Coroutine_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Coroutine_Access,
      Element_Type    => Mapped_Data,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type EPoll_Coroutine_Manager is new Coroutine_Manager with record
      epfd : FD;
      Map  : Coroutine_Maps.Map;
   end record;

   overriding procedure Next_To_Run
     (Self  : in out EPoll_Coroutine_Manager;
      Value : out Coroutine_Access);

   Manager : aliased EPoll_Coroutine_Manager;

   function epoll_create1
     (Flag : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "epoll_create1";

   type epoll_event is record
      events : Interfaces.Unsigned_32;
      data : Coroutine_Access;
   end record
     with Convention => C, Pack;

   function epoll_ctl
     (epfd  : Interfaces.C.int;
      op    : Interfaces.C.int;
      fd    : Interfaces.C.int;
      event : epoll_event) return Interfaces.C.int
     with Import, Convention => C, External_Name => "epoll_ctl";

   function epoll_wait
     (epfd      : Interfaces.C.int;
      events    : access epoll_event;
      maxevents : Interfaces.C.int := 1;
      timeout   : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "epoll_wait";

   EPOLL_CLOEXEC : constant := 8#02000000#;

   pragma Warnings (Off);
   EPOLLIN     : constant := 16#001#;
   EPOLLPRI    : constant := 16#002#;
   EPOLLOUT    : constant := 16#004#;
   EPOLLRDNORM : constant := 16#040#;
   EPOLLRDBAND : constant := 16#080#;
   EPOLLWRNORM : constant := 16#100#;
   EPOLLWRBAND : constant := 16#200#;
   EPOLLMSG    : constant := 16#400#;
   EPOLLERR    : constant := 16#008#;
   EPOLLHUP    : constant := 16#010#;
   EPOLLRDHUP  : constant := 16#2000#;
   --     EPOLLEXCLUSIVE = 1u << 28,
   EPOLLEXCLUSIVE : constant := 16#1000_0000#;
   --     EPOLLWAKEUP = 1u << 29,
   EPOLLWAKEUP    : constant := 16#2000_0000#;
   --     EPOLLONESHOT = 1u << 30,
   EPOLLONESHOT   : constant := 16#4000_0000#;
   --     EPOLLET = 1u << 31
   EPOLLET        : constant := 16#8000_0000#;

   EPOLL_CTL_ADD : constant := 1;
   EPOLL_CTL_DEL : constant := 2;
   EPOLL_CTL_MOD : constant := 3;
   pragma Warnings (On);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Manager (Manager'Access);
      Manager.epfd := epoll_create1 (EPOLL_CLOEXEC);
   end Initialize;

   -----------------
   -- Next_To_Run --
   -----------------

   overriding procedure Next_To_Run
     (Self  : in out EPoll_Coroutine_Manager;
      Value : out Coroutine_Access)
   is
      use type Interfaces.C.int;

      Result : Interfaces.C.int;
      Event  : aliased epoll_event;
   begin
      Value := null;

      if not Self.Map.Is_Empty then
         Result := epoll_wait
           (epfd      => Self.epfd,
            events    => Event'Access,
            maxevents => 1,
            timeout   => 1);

         if Result = 1 then
            Value := Event.data;
         end if;
      end if;
   end Next_To_Run;

   ----------------
   -- Stop_Watch --
   ----------------

   procedure Stop_Watch (Descriptor : FD) is
   begin
      null;
   end Stop_Watch;

   -----------
   -- Watch --
   -----------

   procedure Watch
     (Descriptor : FD;
      Event      : EPoll.Event;
      Mode       : EPoll.Mode)
   is
      use type Interfaces.C.int;
      use type Interfaces.Unsigned_32;
      Mapping : constant array (EPoll.Event) of Interfaces.Unsigned_32 :=
        (Input   => EPOLLIN,
         Output  => EPOLLOUT,
         Error   => EPOLLERR,
         Close   => EPOLLHUP);
      Value  : constant Coroutine_Access := Current_Coroutine;
      Ev     : constant epoll_event :=
        (Mapping (Event) + (if Mode = Edge then EPOLLET else 0),
         Value);
      Result : Interfaces.C.int;
   begin
      Manager.Map.Insert
        (Value,
         (Descriptor => Descriptor,
          Event      => Event,
          Mode       => Mode));

      Result := epoll_ctl
        (epfd  => Manager.epfd,
         op    => EPOLL_CTL_ADD,
         fd    => Descriptor,
         event => Ev);

      pragma Assert (Result = 0);
   end Watch;

end Coroutines.EPoll;
