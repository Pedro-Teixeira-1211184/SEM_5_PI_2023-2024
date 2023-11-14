import mongoose from 'mongoose';
import {IRoomPersistence} from '../../dataschema/IRoomPersistence';

const RoomSchema = new mongoose.Schema(
    {
        roomID: {type: String, unique: true},
        roomFloorCode: {type: String, required: true},
        roomDesignation: {type: String},
        roomName: {type: String, unique: true, required: true}
    },
    {
        timestamps: true
    }
);


RoomSchema.index({roomID: 1}, {unique: true});
export default mongoose.model<IRoomPersistence & mongoose.Document>('Room', RoomSchema);

