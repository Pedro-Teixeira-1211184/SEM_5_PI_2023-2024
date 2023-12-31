import {Request, Response, NextFunction} from 'express';


export default interface IRobotController {
  createRobot(req: Request, res: Response, next: NextFunction);

  updateRobot(req: Request, res: Response, next: NextFunction);

  createRobotType(req: Request, res: Response, next: NextFunction);

  getAllRobots(req: Request, res: Response, next: NextFunction);

  getRobotType(req: Request, res: Response, next: NextFunction);

  getRobotByCode(req: Request, res: Response, next: NextFunction);
}
