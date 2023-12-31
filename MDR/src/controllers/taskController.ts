import {Inject, Service} from 'typedi';
import config from "../../config";
import {NextFunction, Request, Response} from "express";
import ITaskController from "./IControllers/ITaskController";
import ITaskService from "../services/IServices/ITaskService";
import ITaskTypeDTO from "../dto/ITaskTypeDTO";

@Service()
export default class TaskController implements ITaskController /* TODO: extends ../core/infra/BaseController */ {
  constructor(
    @Inject(config.services.task.name) private taskServiceInstance: ITaskService
  ) {
  }

  public async createTaskType(req: Request, res: Response, next: NextFunction) {
    try {
      const result = await this.taskServiceInstance.createTaskType(req.body as ITaskTypeDTO);
      if (result.isFailure) {
        return res.status(400).json(result.errorValue());
      }

      return res.status(201).json(result.getValue());

    } catch (e) {
      return next(e);
    }
  }

  public async createTaskRequest(req: Request, res: Response, next: NextFunction) {
  }

  public async getAllTaskTypes(req: Request, res: Response, next: NextFunction) {
    try {
      const result = await this.taskServiceInstance.getAllTaskTypes();
      if (result.isFailure) {
        return res.status(400).json(result.errorValue());
      }

      return res.status(200).json(result.getValue());
    }catch (e) {
      return next(e);
    }
  }


}
