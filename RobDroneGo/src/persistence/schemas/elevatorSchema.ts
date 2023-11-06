import mongoose from 'mongoose';
import { IElevatorPersistence } from '../../dataschema/IElevatorPersistence';

const ElevatorSchema = new mongoose.Schema(
    {
        elevatorID: { type: String, unique: true },
        elevatorCoordenates: { type: String },
        elevatorBuildingCode: { type: String, required: true },
        elevatorFloorNumbers: { type: String, required: true }
    },
    {
        timestamps: true
    }
);

ElevatorSchema.index({ elevatorID: 1 }, { unique: true });
export default mongoose.model<IElevatorPersistence & mongoose.Document>('Elevator', ElevatorSchema);
