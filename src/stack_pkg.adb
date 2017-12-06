with Ada.Unchecked_Deallocation;
package body stack_pkg is
   procedure Free is new Ada.Unchecked_Deallocation(StackNode, Stack);
   
   function is_Empty(S: Stack) return Boolean is
     (S = null);
   
   function top(S: Stack) return ItemType is
     ( S.Item);
     
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
   

   procedure push(Item: ItemType; S : in out Stack)
   is
   begin
      if not is_Full(s) then
         S := new StackNode'(Item, S); 
      else
         raise Stack_Full;
      end if;
   end push;
   
   procedure pop(S : in out Stack)
   is
      t : Stack := S;
   begin
      S := S.Next;
      Free(t);
   end pop;   
end stack_pkg;
