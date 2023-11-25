import {Request, Response, NextFunction} from "express";

export default interface IElevatorController {
  createElevator(req: Request, res: Response, next: NextFunction);

  findElevatorsByBuildingCode(req: Request, res: Response, next: NextFunction);

  getAll(req: Request, res: Response, next: NextFunction);
}
