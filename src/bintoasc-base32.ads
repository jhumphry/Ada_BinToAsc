-- BinToAsc.Base32
-- Binary data to ASCII codecs - Base64 codec as in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

generic
   Alphabet : Alphabet_32;
   Padding : Character;
   Case_Sensitive : Boolean;
   Allow_Homoglyphs : Boolean;
package BinToAsc.Base32 is

   type Base32_To_String is new Codec_To_String with private;

   overriding function Empty (C : in Base32_To_String) return Boolean;

   overriding
   procedure Reset (C : out Base32_To_String)
     with Post => (C.State = Ready and Empty(C));

   pragma Warnings (GNATprove, Off, "unused variable ""C""");
   overriding
   function Input_Group_Size (C : in Base32_To_String) return Positive is (5);

   overriding
   function Output_Group_Size (C : in Base32_To_String) return Positive is (8);
   pragma Warnings (GNATprove, On, "unused variable ""C""");

   overriding
   procedure Process (C : in out Base32_To_String;
                      Input : in Bin;
                      Output : out String;
                      Output_Length : out Natural);

   overriding
   procedure Process (C : in out Base32_To_String;
                      Input : in Bin_Array;
                      Output : out String;
                      Output_Length : out Natural);

   overriding
   procedure Complete (C : in out Base32_To_String;
                        Output : out String;
                        Output_Length : out Natural);

   function To_String (Input : in Bin_Array) return String
     with Pre => (Input'Length < ((Integer'Last/8 -  1) * 5));

   type Base32_To_Bin is new Codec_To_Bin with private;

   overriding function Empty (C : in Base32_To_Bin) return Boolean;

   overriding
   procedure Reset (C : out Base32_To_Bin)
     with Post => (C.State = Ready and Empty(C));

   pragma Warnings (GNATprove, Off, "unused variable ""C""");
   overriding
   function Input_Group_Size (C : in Base32_To_Bin) return Positive is (8);

   overriding
   function Output_Group_Size (C : in Base32_To_Bin) return Positive is (5);
   pragma Warnings (GNATprove, On, "unused variable ""C""");

   overriding
   procedure Process (C : in out Base32_To_Bin;
                      Input : in Character;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index);

   overriding
   procedure Process (C : in out Base32_To_Bin;
                      Input : in String;
                      Output : out Bin_Array;
                      Output_Length : out Bin_Array_Index);

   overriding
   procedure Complete (C : in out Base32_To_Bin;
                        Output : out Bin_Array;
                        Output_Length : out Bin_Array_Index);

   function To_Bin (Input : in String) return Bin_Array
     with Pre => (Input'Length / 8  < (Bin_Array_Index'Last/5 -  1) and
                      Input'Last < Integer'Last);

private

   type Base32_Bin_Index is range 0..4;
   type Base32_Bin_Buffer is array (Base32_Bin_Index) of Bin;

   type Base32_To_String is new Codec_To_String with
      record
         Next_Index : Base32_Bin_Index := 0;
         Buffer : Base32_Bin_Buffer := (others => 0);
      end record;

   overriding function Empty (C : in Base32_To_String) return Boolean
   is (C.Next_Index = 0);

   type Base32_Reverse_Index is range 0..7;
   type Base32_Reverse_Buffer is array (Base32_Reverse_Index) of Bin;

   type Base32_To_Bin is new Codec_To_Bin with
      record
         Next_Index : Base32_Reverse_Index := 0;
         Buffer : Base32_Reverse_Buffer := (others => 0);
         Padding_Length : Bin_Array_Index := 0;
      end record;

   overriding function Empty (C : in Base32_To_Bin) return Boolean
   is (C.Next_Index = 0);

end BinToAsc.Base32;
