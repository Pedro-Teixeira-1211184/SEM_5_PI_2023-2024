import mongoose from 'mongoose';
import {IRoomPersistence} from '../../dataschema/IRoomPersistence';

const RoomSchema = new mongoose.Schema(
  {
    roomID: {type: String, unique: true},
    roomFloorID: {type: String},
    roomDesignation: {type: String},
    roomName: {type: String, unique: true},
    roomLocalization: {type: String},
  },
  {
    timestamps: true
  }
);


RoomSchema.index({roomID: 1}, {unique: true});
export default mongoose.model<IRoomPersistence & mongoose.Document>('Room', RoomSchema);

