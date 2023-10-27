import mongoose from 'mongoose';
import {IRoomPersistence} from '../../dataschema/IRoomPersistence';

const RoomSchema = new mongoose.Schema(
  {
    RoomID: {type: String, unique: true},
    RoomFloorID: {type: String},
    RoomDesignation: {type: String, unique: true},
    RoomName: {type: String, unique: true},
    RoomLocalization: {type: String},
  },
  {
    timestamps: true
  }
);


RoomSchema.index({roomID: 1}, {unique: true});
export default mongoose.model<IRoomPersistence & mongoose.Document>('Room', RoomSchema);

