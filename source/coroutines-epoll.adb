--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Coroutines.EPoll is

   type EPoll_Coroutine_Manager is new Coroutine_Manager with record
      epfd  : FD;
      Total : Natural := 0;
   end record;

   overriding procedure Next_To_Run
     (Self  : in out EPoll_Coroutine_Manager;
      Value : out Coroutine_Access);

   Manager : aliased EPoll_Coroutine_Manager;

   function epoll_create1
     (Flag : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "epoll_create1";

   type epoll_event is record
      events : Event_Kind_Set;
      data   : Coroutine_Access;
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

   Mapping : constant array (EPoll.Event_Kind) of Event_Kind_Set :=
     (Input   => EPOLLIN,
      Output  => EPOLLOUT,
      Error   => EPOLLERR,
      Close   => EPOLLHUP);

   ---------
   -- "+" --
   ---------

   function "+" (Left : Event_Kind) return Event_Kind_Set is
   begin
      return Mapping (Left);
   end "+";

   function "+"
     (Left : Event_Kind_Set; Right : Event_Kind) return Event_Kind_Set is
   begin
      return Left or (+Right);
   end "+";

   -----------
   -- "and" --
   -----------

   function "and" (Left : Event_Kind_Set; Right : Event_Kind) return Boolean is
   begin
      return (Left and (+Right)) /= 0;
   end "and";

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self  : in out Event) is
   begin
      if Self.Enabled then
         Self.Track (False);
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Manager (Manager'Access);
      Manager.epfd := epoll_create1 (EPOLL_CLOEXEC);
      Manager.Total := 0;
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

      if Self.Total > 0 then
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

   -----------
   -- Track --
   -----------

   procedure Track
     (Self  : in out Event'Class;
      Value : Boolean)
   is
      use type Interfaces.C.int;

      Current  : constant Coroutine_Access := Current_Coroutine;
      Ev       : constant epoll_event :=
        (Self.Events + (if Self.Mode = Edge then EPOLLET else 0),
         Current);
      Result : Interfaces.C.int;
   begin
      if Self.Enabled = Value then
         return;
      end if;

      if Value then
         Manager.Total := Manager.Total + 1;

         Result := epoll_ctl
           (epfd  => Manager.epfd,
            op    => EPOLL_CTL_ADD,
            fd    => Self.FD,
            event => Ev);
      else
         Manager.Total := Manager.Total - 1;

         Result := epoll_ctl
           (epfd  => Manager.epfd,
            op    => EPOLL_CTL_DEL,
            fd    => Self.FD,
            event => Ev);
      end if;

      pragma Assert (Result = 0);
      Self.Enabled := Value;
   end Track;

end Coroutines.EPoll;
