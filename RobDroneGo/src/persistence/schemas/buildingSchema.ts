import mongoose from 'mongoose';
import { IBuildingPersistence } from '../../dataschema/IBuildingPersistence';

const BuildingSchema = new mongoose.Schema(
    {
        buildingID: { type: String, unique: true },
        buildingName: { type: String, unique: true },
        buildingDimensions: { type: String },
        buildingDesignation: { type: String },
        buildingDescription: { type: String }
    },
    {
        timestamps: true
    }
);


BuildingSchema.index({ buildingID: 1 }, { unique: true });
export default mongoose.model<IBuildingPersistence & mongoose.Document>('Building', BuildingSchema);
