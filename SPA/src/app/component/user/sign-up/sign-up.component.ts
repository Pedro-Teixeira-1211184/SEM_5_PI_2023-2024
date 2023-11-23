import {Component, inject, OnInit} from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {RoleService} from "../../../services/role/role.service";
import {AuthService} from "../../../services/auth.service";
import IRoleDTO from "../../../dto/IRoleDTO";
import {MatDialog} from "@angular/material/dialog";

@Component({
  selector: 'app-sign-up',
  templateUrl: './sign-up.component.html',
  styleUrls: ['./sign-up.component.css']
})
export class SignUpComponent implements OnInit {

  ngOnInit(): void {
    this.form = new FormGroup({
      firstName: new FormControl('', Validators.required),
      lastName: new FormControl('', Validators.required),
      email: new FormControl('', [Validators.required, Validators.email]),
      password: new FormControl('', Validators.required),
      role: new FormControl('', Validators.required)
    });
  }

  r_service: RoleService = inject(RoleService);
  service: AuthService = inject(AuthService);

  form!: FormGroup;
  roles: IRoleDTO[] = [];
  conditions = true;

  constructor(private dialog: MatDialog) {
    this.getAllRoles();
  }

  public async getAllRoles(): Promise<void> {
    try {
      this.roles = await this.r_service.getAllRoles();
    } catch (e) {
      console.log(e);
    }
  }

  get firstName() {
    return this.form.get('firstName');
  }

  get lastName() {
    return this.form.get('lastName');
  }

  get email() {
    return this.form.get('email');
  }

  get password() {
    return this.form.get('password');
  }

  get role() {
    return this.form.get('role');
  }

  public async submit() {
    await this.service.signUp(this.firstName?.value, this.lastName?.value, this.email?.value, this.password?.value, this.role?.value);
  }

  accept() {
    this.conditions = false;
  }

  decline() {
    window.location.href = "/home";
  }
}
