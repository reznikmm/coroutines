--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;

package body Coroutines.Timeouts is

   type Timeout_Manager;

   type Timeout_Manager_Access is access all Timeout_Manager'Class;

   type Timeout_Event is new Event_Object with record
      Time    : Ada.Calendar.Time;
      Context : Coroutines.Context;
      Manager : Timeout_Manager_Access;
      Ready   : Boolean := False;
   end record;

   overriding procedure Activate (Self : in out Timeout_Event);

   overriding function Ready (Self : Timeout_Event) return Boolean;

   overriding procedure Deactivate (Self : in out Timeout_Event);

   type Timeout_Event_Access is access all Timeout_Event;

   use type Ada.Calendar.Time;
   function Equal (Left, Right : Timeout_Event_Access) return Boolean is
     (Left.Context = Right.Context and then Left.Time = Right.Time);

   function Less (Left, Right : Timeout_Event_Access) return Boolean;

   package Event_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Timeout_Event_Access,
      "<"          => Less,
      "="          => Equal);

   package Event_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Timeout_Event_Access);

   type Timeout_Manager is new Coroutine_Manager with record
      Active   : Event_Sets.Set;
      Unused   : Event_Vectors.Vector;
      Down     : Coroutine_Manager_Access;
   end record;

   overriding procedure Get_Always_Ready_Event
     (Self   : in out Timeout_Manager;
      Result : out Event_Id);

   overriding procedure New_Round
     (Self    : in out Timeout_Manager;
      Queue   : in out Context_Vectors.Vector;
      Timeout : Duration);

   Manager : aliased Timeout_Manager;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Self : in out Timeout_Event) is
   begin
      if not Self.Manager.Active.Contains (Self'Unchecked_Access) then
         Self.Manager.Active.Insert (Self'Unchecked_Access);
      end if;
   end Activate;

   ----------------
   -- Deactivate --
   ----------------

   overriding procedure Deactivate (Self : in out Timeout_Event) is
   begin
      if Self.Manager.Active.Contains (Self'Unchecked_Access) then
         Self.Manager.Active.Delete (Self'Unchecked_Access);
      end if;

      Self.Manager.Unused.Append (Self'Unchecked_Access);
   end Deactivate;

   ----------------------------
   -- Get_Always_Ready_Event --
   ----------------------------

   procedure Get_Always_Ready_Event
     (Self   : in out Timeout_Manager;
      Result : out Event_Id)
   is
      pragma Unreferenced (Self);
   begin
      Result := Timeout (0.0);
   end Get_Always_Ready_Event;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Manager.Down := Coroutines.Manager;
      Coroutines.Manager := Manager'Access;
   end Initialize;

   ----------
   -- Less --
   ----------

   function Less (Left, Right : Timeout_Event_Access) return Boolean is
      function Less_Context return Boolean;

      ------------------
      -- Less_Context --
      ------------------

      function Less_Context return Boolean is
         use type System.Storage_Elements.Integer_Address;

         L : constant System.Storage_Elements.Integer_Address :=
           System.Storage_Elements.To_Integer (System.Address (Left.Context));
         R : constant System.Storage_Elements.Integer_Address :=
           System.Storage_Elements.To_Integer (System.Address (Right.Context));
      begin
         return L < R;
      end Less_Context;

   begin
      return Left.Time < Right.Time
        or else (Left.Time = Right.Time and then Less_Context);
   end Less;

   ---------------
   -- New_Round --
   ---------------

   procedure New_Round
     (Self    : in out Timeout_Manager;
      Queue   : in out Context_Vectors.Vector;
      Timeout : Duration)
   is
      Limit : Duration := Timeout;
      First : Timeout_Event_Access;
      Now   : Ada.Calendar.Time;
   begin
      if not Self.Active.Is_Empty then
         Now := Ada.Calendar.Clock;

         while not Self.Active.Is_Empty loop
            First := Self.Active.First_Element;

            if First.Time <= Now then
               Queue.Append (First.Context);
               Self.Active.Delete_First;
               Limit := 0.0;
            else
               Limit := Duration'Min (Timeout, First.Time - Now);
               exit;
            end if;
         end loop;
      end if;

      if Self.Down /= null then
         Self.Down.New_Round (Queue, Timeout => Limit);
      elsif Queue.Is_Empty then
         delay Limit;
      end if;
   end New_Round;

   -----------
   -- Ready --
   -----------

   overriding function Ready (Self : Timeout_Event) return Boolean is
   begin
      return Self.Ready;
   end Ready;

   -------------
   -- Timeout --
   -------------

   function Timeout (Value : Duration) return Event_Id is
--      use type Ada.Calendar.Time;
   begin
      return Timeout (Ada.Calendar.Clock + Value);
   end Timeout;

   -------------
   -- Timeout --
   -------------

   function Timeout (Value : Ada.Calendar.Time) return Event_Id is
      Result : Timeout_Event_Access;
   begin
      if Manager.Unused.Is_Empty then
         Result := new Timeout_Event;
      else
         Result := Manager.Unused.Last_Element;
         Manager.Unused.Delete_Last;
      end if;

      Result.all := (Event_Object with
                       Time    => Value,
                       Context => Current_Context,
                       Manager => Manager'Access,
                       Ready   => False);

      return Event_Id (Result);
   end Timeout;

end Coroutines.Timeouts;
