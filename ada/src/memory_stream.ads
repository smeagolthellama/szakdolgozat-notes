-- $Id: memory_stream.ads,v 1.1 2000/11/26 05:00:18 wwg Exp $
-- (c) Warren W. Gay VE3WWG ve3wwg@home.com, ve3wwg@yahoo.com
--
-- Protected under the GNU GPL License

with Ada.Finalization, Ada.Streams;
use  Ada.Finalization, Ada.Streams;

package Memory_Stream is

    type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;
    type Memory_Buffer(Max_Elem: Stream_Element_Offset) is new Controlled with private;

    --------------------------------------------------
    -- The new Stream Type, which must be derived from
    -- Root_Stream_Type. Note that Root_Stream_Type is
    -- NOT derived from Controlled, so if
    -- controlled attributes are necessary, they must
    -- be defined separately, and embedded into this
    -- object, as is done with Memory_Buffer here.
    --------------------------------------------------
    type Memory_Buffer_Stream(Max_Elem: Stream_Element_Offset) is new Root_Stream_Type with record
            Mem_Buf:        Memory_Buffer(Max_Elem);   -- Object with Finalization
    end record;

    type Memory_Buffer_Stream_Ptr is access all Memory_Buffer_Stream;

    -- The overloaded abstract for Read
    procedure Read(Stream: in out Memory_Buffer_Stream;
        Item: out Stream_Element_Array; Last: out Stream_Element_Offset);

    -- The overloaded abstract for Write
    procedure Write(Stream: in out Memory_Buffer_Stream;
        Item: in Stream_Element_Array);

    -- Rewind the Read Memory Buffer Index
    procedure Rewind_Read(Stream: Stream_Access);

    -- Rewind the Write Memory Buffer Index
    procedure Rewind_Write(Stream: Stream_Access);

    -- To permit easy destruction of this stream
    procedure Free(Stream: Stream_Access);

private

    --------------------------------------------------
    -- To create a Memory_Buffer stream with an
    -- Initialize procedure, it must be derived from
    -- a Controlled type. Unfortunately, the type
    -- Root_Stream_Type is not derived from the
    -- Controlled type, so it is done privately here.
    --------------------------------------------------
    type Memory_Buffer(Max_Elem: Stream_Element_Offset) is new Controlled with record
            Read_Offset:    Stream_Element_Offset;
            Write_Offset:   Stream_Element_Offset;
            Buffer:         Stream_Element_Array(1..Max_Elem);
    end record;

    procedure Initialize(Buf: in out Memory_Buffer);
    procedure Write(Buf: in out Memory_Buffer; Item: in Stream_Element_Array);
    procedure Read(Buf: in out Memory_Buffer;
        Item: out Stream_Element_Array;
        Last: out Stream_Element_Offset);
    procedure Rewind_Read(Buf: in out Memory_Buffer);
    procedure Rewind_Write(Buf: in out Memory_Buffer);

end Memory_Stream;
