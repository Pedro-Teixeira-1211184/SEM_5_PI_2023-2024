import { Request, Response, NextFunction } from 'express';

export default interface IUserController  {
  signUp(req: Request, res: Response, next: NextFunction);
  signIn(req: Request, res: Response, next: NextFunction);
  isSignedIn(req: Request, res: Response, next: NextFunction);
  signOut(req: Request, res: Response, next: NextFunction);
}
