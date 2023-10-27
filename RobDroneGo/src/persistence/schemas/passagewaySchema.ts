import mongoose from 'mongoose';
import {IPassagewayPersistence} from '../../dataschema/IPassagewayPersistence';

const PassagewaySchema = new mongoose.Schema(
  {
    passagewayID: {type: String, unique: true},
    passagewayFloorID1: {type: String, required: true},
    passagewayFloorID2: {type: String, required: true},
    passagewayLocalization1: {type: String, required: true},
    passagewayLocalization2: {type: String, required: true}
  },
  {
    timestamps: true
  }
);


PassagewaySchema.index({passagewayID: 1}, {unique: true});
export default mongoose.model<IPassagewayPersistence & mongoose.Document>('Passageway', PassagewaySchema);
