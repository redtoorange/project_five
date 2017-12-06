with Ada.Unchecked_Deallocation;
package body queue_pkg is
   procedure Free is new Ada.Unchecked_Deallocation(QueueNode, QueueNodePointer);

   function  is_Empty(Q: Queue) return Boolean is
     (Q.Front = NULL);

   
   function  is_Full(Q: Queue) return Boolean
   is
      t : QueueNodePointer;
   begin
      t := new QueueNode;
      Free (t);
      return False;
   exception
      when STORAGE_ERROR =>
         return TRUE;
   end is_Full;
   
   
   function  front(Q: Queue) return ItemType
   is
   begin
      if not is_Empty(Q) then
         return Q.Front.all.Data;
      else
         raise Queue_Empty;
      end if;
   end front;

   procedure enqueue (Item: ItemType; Q: in out Queue)
   is
      t : QueueNodePointer := new QueueNode'(Item, Q.Back, Null);
   begin
      if is_Empty(Q) then
         Q.Front := t;
         Q.Back  := Q.Front;
      elsif not is_Full(Q) then
         Q.back.all.next := t;
         Q.back := t;
      else
         raise Queue_Full;
      end if;
   end enqueue;
   
   procedure dequeue (Q: in out Queue)
   is
      t : QueueNodePointer := Q.Front;
   begin
      if is_Empty(Q) then
         raise Queue_Empty;
      else
         Q.Front := Q.Front.all.next;
         
         if Q.Front = Null then
            Q.Back := Null;
         else
            Q.Front.all.Prev := Null;
         end if;   
         
         Free(t);
      end if;
   end dequeue;
end queue_pkg;
