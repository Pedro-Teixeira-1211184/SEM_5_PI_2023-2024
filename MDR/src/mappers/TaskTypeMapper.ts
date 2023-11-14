import {Mapper} from "../core/infra/Mapper";
import {Document, Model} from 'mongoose';

import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {TaskType} from "../domain/taskType";
import {ITaskTypePersistence} from "../dataschema/ITaskTypePersistence";
import ITaskTypeDTO from "../dto/ITaskTypeDTO";

export class TaskTypeMapper extends Mapper<TaskType> {

    public static toDTO(taskType: TaskType): ITaskTypeDTO {
        return {
            id: taskType.id.toString(),
            designation: taskType.designation,
            robotType: taskType.robotType
        } as ITaskTypeDTO;
    }


    public static toDomain(raw: any | Model<ITaskTypePersistence & Document>): TaskType {
        const typeOrError = TaskType.create({
            id: raw.taskTypeID,
            designation: raw.taskTypeDesignation,
            robotType: raw.taskTypeRobotType
        }, new UniqueEntityID(raw.taskTypeID));

        typeOrError.isFailure ? console.log(typeOrError.error) : '';

        return typeOrError.isSuccess ? typeOrError.getValue() : null;
    }

    public static toPersistence(taskType: TaskType): any {
        return {
            taskTypeID: taskType.id.toString(),
            taskTypeDesignation: taskType.designation,
            taskTypeRobotType: taskType.robotType
        }
    }
}
