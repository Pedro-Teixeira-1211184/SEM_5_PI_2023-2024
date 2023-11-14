import mongoose from 'mongoose';
import { IBuildingPersistence } from '../../dataschema/IBuildingPersistence';

const BuildingSchema = new mongoose.Schema(
    {
        buildingID: { type: String, unique: true },
        buildingCode: { type: String, unique: true },
        buildingDimensions: {
          length: {type: Number, required: true},
          width: {type: Number, required: true}
        },
        buildingName: { type: String },
        buildingDescription: { type: String },
        buildingMaxFloors: { type: Number },
        buildingMinFloors: { type: Number }
    },
    {
        timestamps: true
    }
);


BuildingSchema.index({ buildingID: 1 }, { unique: true });
export default mongoose.model<IBuildingPersistence & mongoose.Document>('Building', BuildingSchema);
