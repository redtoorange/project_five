-- This is the generic specification for a dynamic queue abstract data type.

generic
   type ItemType is private;
package queue_pkg is

   type Queue is limited private;

   Queue_Empty, Queue_Full: exception;

   function  is_Empty(Q: Queue) return Boolean;
   function  is_Full(Q: Queue) return Boolean;

   function  front(Q: Queue) return ItemType;

   procedure enqueue (Item: ItemType; Q: in out Queue);
   procedure dequeue (Q: in out Queue);

private

   type QueueNode;

   type QueueNodePointer is access QueueNode;

   type QueueNode is record
      Data: ItemType;
      Prev: QueueNodePointer;
      Next: QueueNodePointer;
   end record;

   type Queue is record
      Front: QueueNodePointer := NULL;
      Back : QueueNodePointer := NULL;
   end record;

end queue_pkg;
