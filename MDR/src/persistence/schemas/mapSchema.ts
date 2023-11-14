import mongoose from 'mongoose';
import {IMapPersistence} from "../../dataschema/IMapPersistence";

const MapSchema = new mongoose.Schema(
  {
    mapID: {type: String, unique: true},
    mapBuildingCode: {type: String, required: true},
    mapFloorNumber: {type: Number, required: true},
    mapSize: {
      length: {type: Number, required: true},
      width: {type: Number, required: true}
    },
    mapMap: {type: Array, required: true},
    mapRooms: {type: Array},
    mapPassageways: {type: Array},
    mapElevator: {type: Object}
  },
  {
    timestamps: true
  }
);


MapSchema.index({mapID: 1}, {unique: true});
export default mongoose.model<IMapPersistence & mongoose.Document>('Map', MapSchema);
