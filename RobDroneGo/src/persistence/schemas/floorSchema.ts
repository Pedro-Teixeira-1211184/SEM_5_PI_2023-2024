import mongoose from 'mongoose';
import { IFloorPersistence } from '../../dataschema/IFloorPersistence';

const FloorSchema = new mongoose.Schema(
  {
    floorID: { type: String, unique: true },
    floorBuildingCode: { type: String, required: true },
    floorNumber: { type: String, required: true },
    floorDescription: { type: String, required: true },
  },
  {
    timestamps: true
  }
);


FloorSchema.index({ floorID: 1 }, { unique: true });
export default mongoose.model<IFloorPersistence & mongoose.Document>('Floor', FloorSchema);
