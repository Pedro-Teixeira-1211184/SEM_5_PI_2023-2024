import mongoose from 'mongoose';
import {IPassagewayPersistence} from '../../dataschema/IPassagewayPersistence';

const PassagewaySchema = new mongoose.Schema(
  {
    passagewayID: {type: String, unique: true},
    passagewayFloorCode1: {type: String, required: true},
    passagewayFloorCode2: {type: String, required: true},
  },
  {
    timestamps: true
  }
);


PassagewaySchema.index({passagewayID: 1}, {unique: true});
export default mongoose.model<IPassagewayPersistence & mongoose.Document>('Passageway', PassagewaySchema);
