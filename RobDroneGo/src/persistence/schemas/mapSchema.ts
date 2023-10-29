import mongoose from 'mongoose';
import {IMapPersistence} from "../../dataschema/IMapPersistence";

const MapSchema = new mongoose.Schema(
  {
    mapID: { type: String, unique: true },
    mapBuildingCode: { type: String, required: true },
    mapFloorNumber: { type: Number, required: true },
    mapSize: {
      width: { type: Number, required: true },
      height: { type: Number, required: true }
    },
    mapMap: { type: Array, required: true },
    mapRooms: { type: Array, required: true },
    mapPassageways: { type: Array, required: true },
    mapElevator: { type: Object, required: true }
  },
  {
    timestamps: true
  }
);


MapSchema.index({ mapID: 1 }, { unique: true });
export default mongoose.model<IMapPersistence & mongoose.Document>('Map', MapSchema);
