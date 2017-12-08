with Ada.Unchecked_Deallocation;
package body stack_pkg is
   procedure Free is new Ada.Unchecked_Deallocation(StackNode, Stack);
   
   
   ----------------------------------------------------------
   -- Purpose: Check to see if a Stack is empty
   -- Parameters: s: Stack to evaluate
   -- Returns: True if the Stack is empty
   ----------------------------------------------------------
   function is_Empty(S: Stack) return Boolean is
     (S = null);
   
   
   ----------------------------------------------------------
   -- Purpose: Return the top value on the Stack
   -- Parameters: S: Stack to pull the value from
   -- Precondition: S is not empty
   -- Returns: The top value on the Stack, it is NOT removed
   ----------------------------------------------------------
   function top(S: Stack) return ItemType is
     ( if not is_Empty(S) then S.Item else raise Stack_Empty);
     
     
   ----------------------------------------------------------
   -- Purpose: Check to see if a Stack is Full
   -- Parameters: S: Stack to evaluate
   -- Returns: True if the Stack is full
   ----------------------------------------------------------
   function is_Full(S: Stack) return Boolean
   is
      t : Stack;
   begin
      t := new StackNode;
      Free (t);
      return False;
   exception
      when STORAGE_ERROR =>
         return TRUE;
   end is_Full;
   

   ----------------------------------------------------------
   -- Purpose: Push a new value onto the Stack
   -- Parameters: Item: Item to push onto the Stack
   --                S: Stack to push onto
   ----------------------------------------------------------
   procedure push(Item: ItemType; S : in out Stack)
   is
   begin
      if not is_Full(s) then
         S := new StackNode'(Item, S); 
      else
         raise Stack_Full;
      end if;
   end push;
   
   
   ----------------------------------------------------------
   -- Purpose: Pop the top value off of a Stack.
   -- Parameters: S: Stack to pop the value off of
   -- Precondition: S is not empty
   ----------------------------------------------------------
   procedure pop(S : in out Stack)
   is
      t : Stack := S;
   begin
      if not is_Empty(S) then
         S := S.Next;
         Free(t);
      else
         raise Stack_Empty;
      end if;
   end pop;   
end stack_pkg;
