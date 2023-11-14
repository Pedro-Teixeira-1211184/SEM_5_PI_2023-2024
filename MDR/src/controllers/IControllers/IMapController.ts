import {Request, Response, NextFunction} from 'express';


export default interface IMapController {
  createMap(req: Request, res: Response, next: NextFunction);
}
