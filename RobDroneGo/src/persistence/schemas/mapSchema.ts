import mongoose from 'mongoose';
import {IMapPersistence} from "../../dataschema/IMapPersistence";

const MapSchema = new mongoose.Schema(
  {
    mapID: { type: String, unique: true },
    mapFloorID: { type: String, unique: true, required: true },
  },
  {
    timestamps: true
  }
);


MapSchema.index({ mapID: 1 }, { unique: true });
export default mongoose.model<IMapPersistence & mongoose.Document>('Map', MapSchema);
