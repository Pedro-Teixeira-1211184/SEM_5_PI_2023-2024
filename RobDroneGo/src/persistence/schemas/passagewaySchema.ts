import mongoose from 'mongoose';
import { IPassagewayPersistence } from '../../dataschema/IPassagewayPersistence';

const PassagewaySchema = new mongoose.Schema(
    {
        id: { type: String, unique: true },
        floorID1: { type: String},
        floorID2: { type: String},
        localization1: { type: String},
        localization2: { type: String},
    },
    {
        timestamps: true
    }
);


PassagewaySchema.index({ passagewayID: 1 }, { unique: true });
export default mongoose.model<IPassagewayPersistence & mongoose.Document>('Passageway', PassagewaySchema);