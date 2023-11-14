import mongoose from 'mongoose';
import {ITaskTypePersistence} from "../../dataschema/ITaskTypePersistence";


const TaskTypeSchema = new mongoose.Schema(
    {
        taskTypeID: {type: String, unique: true},
        taskTypeDesignation: {type: String, required: true, unique: true},
        taskTypeRobotType: {type: String, required: true}
    },
    {
        timestamps: true
    }
);


TaskTypeSchema.index({ taskTypeID: 1 }, { unique: true });
export default mongoose.model<ITaskTypePersistence & mongoose.Document>('TaskType', TaskTypeSchema);
