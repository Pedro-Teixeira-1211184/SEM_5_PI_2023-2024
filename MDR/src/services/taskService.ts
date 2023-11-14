import {Service, Inject} from 'typedi';
import config from "../../config";
import ITaskService from "./IServices/ITaskService";
import ITaskTypeDTO from "../dto/ITaskTypeDTO";
import {Result} from "../core/logic/Result";
import ITaskTypeRepo from "./IRepos/ITaskTypeRepo";
import {TaskTypeMapper} from "../mappers/TaskTypeMapper";
import {RobotType} from "../domain/robotType";
import {TaskType} from "../domain/taskType";
import IRobotTypeRepo from "./IRepos/IRobotTypeRepo";


@Service()

export default class TaskService implements ITaskService {

    constructor(
        @Inject(config.repos.taskType.name) private taskTypeRepo: ITaskTypeRepo,
        @Inject(config.repos.robotType.name) private robotTypeRepo: IRobotTypeRepo,
    ) {
    }

    public async createTaskType(taskTypeDTO: ITaskTypeDTO): Promise<Result<ITaskTypeDTO>> {
        try {
            const robotType = await this.robotTypeRepo.findByDesignation(taskTypeDTO.robotType);
            if (robotType == null) {
                return Result.fail<ITaskTypeDTO>('Robot type does not exist');
            }
            const typeOrError = await TaskType.create(taskTypeDTO);
            if (typeOrError.isFailure) {
                return Result.fail<ITaskTypeDTO>('Task type is not valid');
            }
            const type = await this.taskTypeRepo.save(typeOrError.getValue());
            if (type == null) {
                return Result.fail<ITaskTypeDTO>('Task type already exists');
            }
            return Result.ok<ITaskTypeDTO>(TaskTypeMapper.toDTO(type));
        } catch (e) {
            throw e;
        }
    }


}
