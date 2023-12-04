import {Component, inject, OnInit} from '@angular/core';
import {AuthService} from "../../../services/auth.service";
import {IUserDTO} from "../../../dto/IUserDTO";

@Component({
  selector: 'app-delete',
  templateUrl: './delete.component.html',
  styleUrls: ['./delete.component.css']
})
export class DeleteComponent implements OnInit {

  service: AuthService = inject(AuthService);
  user!: IUserDTO;

  ngOnInit(): void {
  }

  constructor() {
    this.getUser();
  }

  async getUser() {
    this.user = await this.service.isAuthenticatedUser();
  }

  async deleteAccount() {
    await this.service.deleteUser(this.user.email);
  }

  public async logout(): Promise<void> {
    await this.service.logout();
  }
}
