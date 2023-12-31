import {Inject, Service} from 'typedi';

import {Document, FilterQuery, Model} from "mongoose";
import ITaskTypeRepo from "../services/IRepos/ITaskTypeRepo";
import {ITaskTypePersistence} from "../dataschema/ITaskTypePersistence";
import {TaskType} from "../domain/taskType";
import {IRobotTypePersistence} from "../dataschema/IRobotTypePersistence";
import {TaskTypeMapper} from "../mappers/TaskTypeMapper";


@Service()
export default class TaskTypeRepo implements ITaskTypeRepo {

  constructor(
    @Inject('taskTypeSchema') private taskTypeSchema: Model<ITaskTypePersistence & Document>,
  ) {
  }

  delete(taskType: TaskType): Promise<void> {
    return Promise.resolve(undefined);
  }

  public async exists(taskType: TaskType): Promise<boolean> {
    const query = {taskTypeDesignation: taskType.designation, taskTypeRobotType: taskType.robotType};
    const record = await this.taskTypeSchema.findOne(query as FilterQuery<IRobotTypePersistence & Document>);
    return record == null;
  }

  public async findByName(designation: string): Promise<TaskType> {
    try {
      const query = {taskTypeDesignation: designation};
      return this.taskTypeSchema.findOne(query as FilterQuery<ITaskTypePersistence & Document>)
        .then((type) => {
          if (type == null) return null;
          return TaskTypeMapper.toDomain(type);
        })
    } catch (error) {
      throw error;
    }
  }

  public async save(taskType: TaskType): Promise<TaskType> {
    try {
      if (await this.exists(taskType)) {
        const document = await this.taskTypeSchema.create(TaskTypeMapper.toPersistence(taskType));
        return TaskTypeMapper.toDomain(document);
      } else {
        return null;
      }
    } catch (error) {
      throw error;
    }
  }

  public async findAll(): Promise<TaskType[]> {
    try {
      const types = await this.taskTypeSchema.find({});
      return types.map((type) => TaskTypeMapper.toDomain(type));
    } catch (e) {
      throw e;
    }
  }


}
