import {Request, Response, NextFunction} from 'express';


export default interface ITaskController {
  createTaskType(req: Request, res: Response, next: NextFunction);

  createTaskRequest(req: Request, res: Response, next: NextFunction);

  getAllTaskTypes(req: Request, res: Response, next: NextFunction);
}
