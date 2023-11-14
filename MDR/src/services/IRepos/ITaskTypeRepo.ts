import { Repo } from "../../core/infra/Repo";
import {TaskType} from "../../domain/taskType";


export default interface ITaskTypeRepo extends Repo<TaskType> {
     save(taskType: TaskType): Promise<TaskType>;
     findByName(designation: string): Promise<TaskType>;
     delete(taskType: TaskType): Promise<void>;
     exists(taskType: TaskType): Promise<boolean>;
}
