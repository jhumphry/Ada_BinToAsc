-- BinToAsc
-- Binary data to ASCII codecs

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

generic
   type Bin is mod <>;
   type Bin_Array_Index is (<>);
   type Bin_Array is array (Bin_Array_Index range <>) of Bin;
package BinToAsc is

   type Codec_State is (Ready, Complete, Failed);

   type Codec is abstract tagged record
      State : Codec_State := READY;
   end record;

   type Codec_To_String is abstract new Codec with null record;

   function Maximum_Expansion (C : in Codec_To_String)
                               return Positive is abstract;

   procedure Process (C : in out Codec_To_String;
                      Input : in Bin;
                      Output : out String;
                      Output_Length : out Natural) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= Maximum_Expansion(C));

   procedure Process (C : in out Codec_To_String;
                      Input : in Bin_Array;
                      Output : out String;
                      Output_Length : out Natural) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= Input'Length * Maximum_Expansion(C));

   procedure Completed (C : in out Codec_To_String;
                        Output : out String;
                        Output_Length : out Natural) is abstract
     with Pre'Class => (C.State = Ready and
                          Output'Length >= Maximum_Expansion(C)),
       Post'Class => C.State in Complete | Failed;

   -- Define Alphabet types

   type Alphabet_16 is array (Integer range 0..15) of Character;
   type Alphabet_32 is array (Integer range 0..31) of Character;
   type Alphabet_64 is array (Integer range 0..63) of Character;

   -- This compile-time check is useful for GNAT, but in GNATprove it currently
   -- just generates a warning that it can not yet be proved correct.
   pragma Warnings (GNATprove, Off, "Compile_Time_Error");
   pragma Compile_Time_Error ((Bin'Size /= 8 or
                                (Bin'Pos(Bin'Last)-Bin'Pos(Bin'First))/=255),
                              "BinToAsc only works where the binary type" &
                                "specified is a regular 8-bit byte.");
   pragma Warnings (GNATprove, On, "Compile_Time_Error");

end BinToAsc;
