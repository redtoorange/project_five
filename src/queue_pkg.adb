package body queue_pkg is
begin
   function  is_Empty(Q: Queue) return Boolean
   is
   begin
      return False;
   end is_Empty;
   
   function  is_Full(Q: Queue) return Boolean;
   is
   begin
      return False;
   end is_Full;

   function  front(Q: Queue) return ItemType;
   is
   begin
      return Q.all.
   end front;

   procedure enqueue (Item: ItemType; Q: in out Queue)
   is
   begin
   end enqueue;
   
   procedure dequeue (Q: in out Queue)
   is
   begin
   end dequeue;
end queue_pkg;
