--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Hashed_Maps;

package body Coroutines.Polling is

   use type Interfaces.C.int;

   function epoll_create1
     (Flag : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "epoll_create1";

   pragma Warnings (Off);
   EPOLL_CLOEXEC : constant := 8#02000000#;

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

   type Polling_Manager;

   type Polling_Manager_Access is access all Polling_Manager'Class;

   type Polling_Event is new Event_Object with record
      FD      : Polling.FD;
      Events  : Polling_Kind_Set;
      Context : Coroutines.Context;
      Manager : Polling_Manager_Access;
      Ready   : Boolean := False;
      Active  : Boolean := False;  --  Already in epfd
      Drop    : Boolean := False;  --  Already in epfd, but should not
   end record;

   overriding procedure Activate (Self : in out Polling_Event);

   overriding function Ready (Self : Polling_Event) return Boolean;

   overriding procedure Deactivate (Self : in out Polling_Event);

   type Polling_Event_Access is access all Polling_Event;

   function Hash (Value : FD) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type'Mod (Value));

   pragma Warnings (Off);
   type epoll_event is record
      events : Interfaces.Unsigned_32;
      data   : Polling_Event_Access;
   end record
     with Convention => C, Pack, Size => 12 * 8;
   pragma Warnings (On);

   function epoll_ctl
     (epfd  : Interfaces.C.int;
      op    : Interfaces.C.int;
      fd    : Interfaces.C.int;
      event : epoll_event) return Interfaces.C.int
     with Import, Convention => C, External_Name => "epoll_ctl";

   type epoll_event_array is
     array (Ada.Containers.Count_Type range <>) of epoll_event;

   function epoll_wait
     (epfd      : Interfaces.C.int;
      events    : out epoll_event_array;
      maxevents : Interfaces.C.int;
      timeout   : Interfaces.C.int) return Interfaces.C.int
     with Import, Convention => C, External_Name => "epoll_wait";

   package Event_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => FD,
      Element_Type    => Polling_Event_Access,
      Hash            => Hash,
      Equivalent_Keys => Interfaces.C."=");

   package Event_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Polling_Event_Access);

   type Polling_Manager is new Coroutine_Manager with record
      epfd     : FD;
      Active   : Event_Maps.Map;  --  Events in epfd
      Unused   : Event_Vectors.Vector;  --  Events not in epfd
   end record;

   overriding procedure Get_Always_Ready_Event
     (Self   : in out Polling_Manager;
      Result : out Event_Id);

   overriding procedure New_Round
     (Self    : in out Polling_Manager;
      Queue   : in out Context_Vectors.Vector;
      Timeout : Duration);

   function To_C (Value : Polling_Kind_Set) return Interfaces.Unsigned_32;

   Manager : aliased Polling_Manager;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Self : in out Polling_Event) is
      Error : Interfaces.C.int;
   begin
      if not Self.Active then
         Error := epoll_ctl
           (epfd  => Self.Manager.epfd,
            op    => EPOLL_CTL_ADD,
            fd    => Self.FD,
            event => (To_C (Self.Events), Self'Unchecked_Access));

         pragma Assert (Error = 0);

         Self.Active := True;
         Self.Manager.Active.Insert (Self.FD, Self'Unchecked_Access);
      end if;
   end Activate;

   ----------------
   -- Deactivate --
   ----------------

   overriding procedure Deactivate (Self : in out Polling_Event) is
   begin
      Self.Drop := True;
   end Deactivate;

   ----------------------------
   -- Get_Always_Ready_Event --
   ----------------------------

   overriding procedure Get_Always_Ready_Event
     (Self   : in out Polling_Manager;
      Result : out Event_Id) is
   begin
      raise Constraint_Error;
   end Get_Always_Ready_Event;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      pragma Assert (Coroutines.Manager = null, "Register Pooling first!");
      Coroutines.Manager := Manager'Access;
      Manager.epfd := epoll_create1 (EPOLL_CLOEXEC);
   end Initialize;

   ---------------
   -- New_Round --
   ---------------

   overriding procedure New_Round
     (Self    : in out Polling_Manager;
      Queue   : in out Context_Vectors.Vector;
      Timeout : Duration)
   is
      Error : Interfaces.C.int;
      Last  : constant Positive := Self.Unused.Last_Index + 1;
   begin
      --  Remove unused FD from epfd
      for J of Self.Active loop
         if J.Drop then
            Error := epoll_ctl
              (epfd  => Self.epfd,
               op    => EPOLL_CTL_DEL,
               fd    => J.FD,
               event => (0, null));

            pragma Assert (Error = 0);
            Self.Unused.Append (J);
         end if;
      end loop;

      --  Remove unused FD from Active
      for J in Last .. Self.Unused.Last_Index loop
         Self.Active.Delete (Self.Unused (J).FD);
      end loop;

      declare
         Events : epoll_event_array (1 .. Self.Active.Length);
      begin
         Error := epoll_wait
           (epfd      => Self.epfd,
            events    => Events,
            maxevents => Events'Length,
            timeout   => Interfaces.C.int (1000.0 * Timeout));

         if Error >= 0 then
            for J in 1 .. Ada.Containers.Count_Type (Error) loop
               Queue.Append (Events (J).data.Context);
               Events (J).data.Ready := True;
            end loop;
         end if;
      end;
   end New_Round;

   -----------
   -- Ready --
   -----------

   overriding function Ready (Self : Polling_Event) return Boolean is
   begin
      return Self.Ready;
   end Ready;

   ----------
   -- To_C --
   ----------

   function To_C (Value : Polling_Kind_Set) return Interfaces.Unsigned_32 is
      use type Interfaces.Unsigned_32;

      Mapping : constant array (Polling_Kind) of Interfaces.Unsigned_32 :=
        (Input   => EPOLLIN,
         Output  => EPOLLOUT,
         Error   => EPOLLERR,
         Close   => EPOLLHUP);

      Result  : Interfaces.Unsigned_32 := 0;
   begin
      for J in Value'Range loop
         if Value (J) then
            Result := Result + Mapping (J);
         end if;
      end loop;

      return Result;
   end To_C;

   -----------
   -- Watch --
   -----------

   function Watch (File : FD; Events : Polling_Kind_Set) return Event_Id is
      Cursor : constant Event_Maps.Cursor := Manager.Active.Find (File);
      Result : Polling_Event_Access;
      Error  : Interfaces.C.int;
   begin
      if Event_Maps.Has_Element (Cursor) then
         Result := Event_Maps.Element (Cursor);
         Result.Context := Current_Context;
         Result.Ready := False;
         Result.Drop := False;

         if Result.Events = Events then
            return Event_Id (Result);
         else
            Error := epoll_ctl
              (epfd  => Manager.epfd,
               op    => EPOLL_CTL_DEL,
               fd    => File,
               event => (0, null));

            pragma Assert (Error = 0);
         end if;
      elsif Manager.Unused.Is_Empty then
         Result := new Polling_Event;
      else
         Result := Manager.Unused.Last_Element;
         Manager.Unused.Delete_Last;
      end if;

      Result.all := (Event_Object with
                       FD      => File,
                       Events  => Events,
                       Context => Current_Context,
                       Manager => Manager'Access,
                       Ready   => False,
                       Active  => False,
                       Drop    => False);

      return Event_Id (Result);
   end Watch;

end Coroutines.Polling;
