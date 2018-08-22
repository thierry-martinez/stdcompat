pipeline {
    agent {
        dockerfile {
            label 'slave'
        }
    }

    stages {
        stage('Build') {
            steps {
                sh 'eval `opam config env` && autoreconf && mkdir build && cd build && ../configure && make'
            }
        }
        stage('Test') {
            parallel {
                for (switch_name in ['3.07', '3.08.4', '3.09.3']) {
                    stage(switch_name) {
                        steps {
                            sh "opam switch $(switch_name) && eval `opam config env` && mkdir build/$(switch_name) && cd build/$(switch_name) && ../../configure && make && make tests"
                        }
                    }
                }
            }
        }
        stage('Deploy') {
            steps {
                sh 'cd build && make dist'
                archiveArtifacts artifacts: 'build/*.tar.gz', fingerprint: true
            }
        }
    }
}
