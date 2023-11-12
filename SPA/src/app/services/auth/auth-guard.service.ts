import {inject, Injectable} from '@angular/core';
import {CanActivate, Router} from "@angular/router";
import {AuthService} from "../auth.service";

@Injectable({
  providedIn: 'root'
})
export class AuthGuardService implements CanActivate {

  auth: AuthService = inject(AuthService);

  constructor(private authService: AuthService, private router: Router) {
  }

  async canActivate(): Promise<boolean> {//| UrlTree> | Promise<boolean | UrlTree> | boolean | UrlTree {
    if (await this.auth.isAuthenticated()) {
      return true;
    } else {
      this.router.navigate(['login']);
      return false;
    }
  }
}
