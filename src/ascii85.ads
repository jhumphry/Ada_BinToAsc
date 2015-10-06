-- ASCII85
-- Various binary data to ASCII codecs known as Base85, ASCII85 etc

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with System.Storage_Elements;
with BinToAsc, BinToAsc.Base85;

package ASCII85 is

   package BToA is new BinToAsc(Bin => System.Storage_Elements.Storage_Element,
                                Bin_Array_Index => System.Storage_Elements.Storage_Offset,
                                Bin_Array => System.Storage_Elements.Storage_Array);

   use type BToA.Alphabet;

   subtype Codec_State is BToA.Codec_State;
   function Ready return BToA.Codec_State renames BToA.Ready;
   function Complete return BToA.Codec_State renames BToA.Completed;
   function Failed return BToA.Codec_State renames BToA.Failed;

   Z85_Alphabet : constant BToA.Alphabet_85 :=
     "0123456789" &
     "abcdefghij" &
     "klmnopqrst" &
     "uvwxyzABCD" &
     "EFGHIJKLMN" &
     "OPQRSTUVWX" &
     "YZ.-:+=^!/" &
     "*?&<>()[]{" &
     "}@%$#";

   package Z85 is new BToA.Base85(Alphabet => Z85_Alphabet);

end ASCII85;
