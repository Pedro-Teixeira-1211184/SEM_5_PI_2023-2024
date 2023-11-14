import mongoose from 'mongoose';
import {IRobotPersistence} from '../../dataschema/IRobotPersistence';

const RobotSchema = new mongoose.Schema(
  {
    robotID: {type: String, unique: true},
    robotRobotType: {type: String, required: true},
    robotCode: {type: String, required: true, unique: true},
    robotSerialNumber: {type: String, required: true},
    robotNickname: {type: String, required: true, unique: true},
    robotBrand: {type: String, required: true},
    robotIsActive: {type: Boolean, required: true}
  },
  {
    timestamps: true
  }
);


RobotSchema.index({robotID: 1}, {unique: true});
export default mongoose.model<IRobotPersistence & mongoose.Document>('Robot', RobotSchema);
