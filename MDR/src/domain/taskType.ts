import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import ITaskTypeDTO from "../dto/ITaskTypeDTO";
import {TaskTypeId} from "./taskTypeId";


interface TaskTypeProps {
    designation: string;
    robotType: string;
}

export class TaskType extends AggregateRoot<TaskTypeProps> {
    get id(): UniqueEntityID {
        return this._id;
    }

    get taskTypeId(): TaskTypeId {
        return new TaskTypeId(this.taskTypeId.toValue());
    }

    get designation(): string {
        return this.props.designation;
    }

    get robotType(): string {
        return this.props.robotType;
    }

    private constructor(props: TaskTypeProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create(taskTypeDTO: ITaskTypeDTO, id?: UniqueEntityID): Result<TaskType> {
        const designation = taskTypeDTO.designation;


        const type = new TaskType({
            designation: designation,
            robotType: taskTypeDTO.robotType,
        }, id);
        return Result.ok<TaskType>(type);
    }

}

