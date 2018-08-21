pipeline {
    agent {
        dockerfile {
            label 'slave'
        }
    }

    stages {
        stage('Build') {
            steps {
                sh 'eval `opam config env` && autoreconf && ./configure && make'
            }
        }
        stage('Test') {
            steps {
                sh './test_all_switches.sh'
            }
        }
        stage('Deploy') {
            steps {
                sh 'make dist'
                archiveArtifacts artifacts: '*.tar.gz', fingerprint: true
            }
        }
    }
}
