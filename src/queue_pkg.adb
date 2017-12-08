with Ada.Unchecked_Deallocation;
package body queue_pkg is
   procedure Free is new Ada.Unchecked_Deallocation(QueueNode, QueueNodePointer);


   ----------------------------------------------------------
   -- Purpose: Check to see if a Queue is empty
   -- Parameters: Q: Queue to evaluate
   -- Returns: True if the Queue is empty
   ----------------------------------------------------------
   function  is_Empty(Q: Queue) return Boolean is
     (Q.Front = NULL);

   
   ----------------------------------------------------------
   -- Purpose: Check to see if a Queue is Full
   -- Parameters: Q: Queue to evaluate
   -- Returns: True if the Queue is full
   ----------------------------------------------------------
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
   
   
   ----------------------------------------------------------
   -- Purpose: Return the front value of the Queue
   -- Parameters: Q: Queue to pull the value from
   -- Precondition: Q is not empty
   -- Returns: The front value on the Queue, it is NOT removed
   ----------------------------------------------------------
   function  front(Q: Queue) return ItemType is
     (if not is_Empty(Q) then Q.Front.Data else raise Queue_Empty);

   
   ----------------------------------------------------------
   -- Purpose: Place a new value at the back of a Queue
   -- Parameters: Item: Item to place in the Queue
   --                Q: Queue to place the value in
   ----------------------------------------------------------
   procedure enqueue (Item: ItemType; Q: in out Queue)
   is
      t : QueueNodePointer := new QueueNode'(Item, Q.Back, Null);
   begin
      if is_Empty(Q) then
         Q.Front := t;
         Q.Back  := Q.Front;
      elsif not is_Full(Q) then
         Q.back.next := t;
         Q.back := t;
      else
         raise Queue_Full;
      end if;
   end enqueue;
   
   
   ----------------------------------------------------------
   -- Purpose: Remove the front value of a Queue.
   -- Parameters: Q: Queue to remove the value from
   -- Precondition: Q is not empty
   ----------------------------------------------------------
   procedure dequeue (Q: in out Queue)
   is
      t : QueueNodePointer := Q.Front;
   begin
      if is_Empty(Q) then
         raise Queue_Empty;
      else
         Q.Front := Q.Front.next;
         
         if Q.Front = Null then
            Q.Back := Null;
         else
            Q.Front.Prev := Null;
         end if;   
         
         Free(t);
      end if;
   end dequeue;
end queue_pkg;
