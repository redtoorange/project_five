-- This is the generic specification for a dynamic stack abstract data type.

generic
   type ItemType is private;
package stack_pkg is

   type Stack is limited private;

   Stack_Empty, Stack_Full: exception;

   function is_Empty(S: Stack) return Boolean;
   function is_Full(S: Stack) return Boolean;

   procedure push(Item: ItemType; S : in out Stack);
   procedure pop(S : in out Stack);

   function top(S: Stack) return ItemType;

private
   type StackNode;

   type Stack is access StackNode;

   type StackNode is record
      Item: ItemType;
      Next: Stack;
   end record;

end stack_pkg;
