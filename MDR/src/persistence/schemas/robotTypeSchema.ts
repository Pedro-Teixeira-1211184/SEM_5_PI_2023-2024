import mongoose from 'mongoose';
import {IRobotTypePersistence} from "../../dataschema/IRobotTypePersistence";


const RobotTypeSchema = new mongoose.Schema(
    {
        robotTypeID: {type: String, unique: true},
        robotTypeDesignation: {type: String, required: true, unique: true}
    },
    {
        timestamps: true
    }
);


RobotTypeSchema.index({ robotTypeID: 1 }, { unique: true });
export default mongoose.model<IRobotTypePersistence & mongoose.Document>('RobotType', RobotTypeSchema);
