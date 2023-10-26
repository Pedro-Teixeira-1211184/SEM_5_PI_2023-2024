import {Request, Response, NextFunction} from 'express';


export default interface IPassagewayController {
  createPassageway(req: Request, res: Response, next: NextFunction);
}