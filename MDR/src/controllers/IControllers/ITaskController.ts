import {Request, Response, NextFunction} from 'express';


export default interface ITaskController {
  createTaskType(req: Request, res: Response, next: NextFunction);
}
